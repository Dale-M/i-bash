#!/usr/bin/guile -s                                 ;; -*-scheme-*-
!#

                                                          
(use-modules (ice-9 regex) (ice-9 popen))



;; Set up global options, reading from the command-line where possible.

(define module-dir (string-append (getenv "HOME") "/i-bash/modules"))
(define listen-port 9081)

(when (> (length (command-line)) 1) (set! module-dir (cadr (command-line))))

(when (> (length (command-line)) 2)
  (set! listen-port (string->number (caddr (command-line)))))



;; Index the library of modules we have at our disposal for processing
;; command lines.

(define db-modules '())
(define verbs '())
(define word-translators '())

;;    Let each module in our local file system introduce itself onto the
;;    db-modules list above.
(let* ((project-dir (string-append module-dir "/"))
       (dir (opendir project-dir)))
  (let loop ((file (readdir dir)))
    (when (not (eof-object? file))
      (or (eq? #\. (string-ref file 0))
          (eq? #\~ (string-ref file (1- (string-length file))))
          (load (string-append project-dir "/" file)))
      (loop (readdir dir)))))

;;    Find the set of verbs and word translations each module provides,
;;    and add to the two global lists above.
(for-each (lambda (module)
            (let ((v (module-variable (resolve-module module) 'verbs)))
              (when v (set! verbs (append! ((variable-ref v)) verbs))))
            (let ((w (module-variable (resolve-module module)
                                      'word-translators)))
              (when w (set! word-translators (append! ((variable-ref w)) 
                                                      word-translators)))))
          db-modules)



;; Run the word-translators over command-words, return pair of new list of
;; command words (translated), and list of used translator s-expressions.

(define (run-translators-0 command-words)
  (let loop-1 ((translators word-translators)
               (translator-stack '())
               (words command-words))
    (cond ((null? translators)
           (cons words (reverse translator-stack)))
          (else
           (let ((translator (primitive-eval (car translators))))
             (let loop-2 ((words command-words)
                          (new-words '())
                          (matched #f))
               (cond ((null? words)
                      (loop-1 (cdr translators)
                              (append (if matched (list (car translators)) '())
                                      translator-stack)
                              (reverse new-words)))
                     (else
                      (let ((new-word (translator (car words))))
                        (loop-2 (cdr words)
                                (cons new-word new-words)
                                (if (string=? new-word (car words))
                                    matched
                                    #t)))))))))))


;; Get the command-words translated and obtain list of used translators,
;; then concoct a new s-expression which will apply the translators again;
;; this is prepended to the incumbent command-stack and returned with the
;; translated command-words.

(define (run-translators-1 command-words command-stack)
  (let* ((res (run-translators-0 command-words))
         (command-words (car res))
         (translator-stack (cdr res)))
    (and (not (null? translator-stack))
         (cons command-words
               (cons `(lambda (command-words command-stack)
                        (let loop ((stack (quote ,translator-stack))
                                   (command-words command-words))
                          (cond ((null? stack)
                                 (cons command-words command-stack))
                                (else
                                 (let ((tr (primitive-eval (car stack))))
                                   (let loop-2 ((words command-words)
                                                (ret '()))
                                     (if (null? words)
                                         (loop (cdr stack) (reverse ret))
                                         (loop-2 (cdr words)
                                                 (cons (tr (car words))
                                                       ret)))))))))
                     command-stack)))))



;;  We accept connections, and process intructions, from one client at a
;;  time.  It is expected that clients will close their connections in a
;;  timely manner otherwise the system will hang for everybody using it.

(let ((socket (socket AF_INET SOCK_STREAM 0)))
  (bind socket AF_INET INADDR_LOOPBACK listen-port)
  (listen socket 5)
  (let socket-listen-loop ()
    (let ((client (car (accept socket))))
      (let client-command-loop ()
        (let ((argument (read client)))
          (and argument
               (not (eof-object? argument))
               (not (null? argument))
               (let* ((command-words (car argument))
                      (command-stack '())   ;; We don't care about this on
                                            ;; the server.
                      (return-stack '())    ;; --This is what we are
                                            ;; interested in building
                                            ;; here.
                      (run-command 
                       (lambda (command)
                         (and command
                              (let ((ret (command command-words command-stack)))
                                (cond (ret
                                       (set! command-words (car ret))
                                       (set! command-stack (cdr ret))))
                                ret)))))
                 
                 (run-command run-translators-1)
                 (set! return-stack command-stack)
                 (set! command-stack '())
                   
                 (let loop ()
                   (cond ((not (null? command-words))
                          (and (let* ((r- (assoc-ref verbs (car command-words)))
                                      (r (and r- (r- command-words command-stack))))
                                 (cond ((run-command (primitive-eval r))
                                        (set! return-stack (cons r return-stack))
                                        #t)
                                       (else #f)))
                               (loop)))))
                 
                 (if (null? return-stack)
                     (write '() client)
                     (write `(lambda (command-words command-stack)
                               (for-each
                                (lambda (proc)
                                  (and proc
                                       (let ((ret ((primitive-eval proc) 
                                                   command-words 
                                                   command-stack)))
                                         (cond (ret
                                                (set! command-words (car ret))
                                                (set! command-stack
                                                      (cdr ret)))))))
                                (quote ,(reverse return-stack)))
                               (cons command-words command-stack))
                            client))

                 (client-command-loop))))))

    (socket-listen-loop)))
