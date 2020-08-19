;;   THE FULL PATH TO THIS FILE SHOULD BE REFERRED TO IN THE ENVIRONMENT
;;   VARIABLE "I_BASH_CALLOUT_2", OR ELSE COPIED TO
;;   "$HOME/i-bash/remote-sender.scm", FOR EFFECT.


;;   The functions in this file are called-out to whenever the user enters
;;   a command on the ibash command line.


;;   Copyright (C) 2020 Dale Mellor
;;
;;   This file is part of iBash, the intelligent Bourne Again SHell.
;;
;;   iBash is free software: you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation, either version 3 of the License, or
;;   (at your option) any later version.
;;
;;   iBash is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU General Public License
;;   along with iBash.  If not, see <http://www.gnu.org/licenses/>.



(define-module (db core)
  #:use-module (ice-9 popen)
  #:export (process))


;; Either return (cons socket action-procedure) or #f.
(define (establish-remote-call)
  (let ((connection (getenv "I_BASH_REMOTE")))
    (and connection
         (let ((split (string-split connection #\:)))
           (cond ((null? (cdr split)) #f)
                 (else
                  (catch 'getaddrinfo-error
                    (lambda ()
                      (let*  ((addr (car (getaddrinfo (car split)
                                                      (cadr split) 0 AF_INET)))
                              (socket (socket (addrinfo:fam addr)
                                              (addrinfo:socktype addr)
                                              (addrinfo:protocol addr))))
                        (connect socket (addrinfo:addr addr))
                        (cons socket
                              (lambda (command-words command-stack)
                                (write (cons command-words command-stack)
                                       socket)
                                (let ((return (read socket)))
                                  (and (not (null? return))
                                       (eval return (current-module))))))))
                    (lambda (key errcode) #f))))))))



;;  The command-words are the tokens of the command line (list of
;;  strings), and the command-stack is a list of command lines *in reverse
;;  order* which are derived by the intelligence from the originals
;;  (command-words).
;;
;;  The return value is the updated command-stack, this being ultimately
;;  sent back to the bash program and executed as if the new commands had
;;  been typed at the terminal.
(define (process command-words command-stack)
  (cond ((establish-remote-call)
         => (lambda (R)
              (let ((server-socket (car R))
                    (remote-call (cdr R))
                    (run-command (lambda (command)
                                   (and command
                                        (let ((ret (command command-words
                                                            command-stack)))
                                          (when ret
                                            (set! command-words (car ret))
                                            (set! command-stack (cdr ret)))
                                          ret)))))
                (let loop ()
                     (unless (null? command-words)
                       (when (run-command (remote-call command-words
                                                       command-stack))
                         (loop))))
                (when server-socket (close server-socket)))))
        (else
         (set! command-words
               (string-split
                "echo REMOTE iBASH INTELLIGENCE SERVICE NOT AVAILABLE"
                #\space))))
  (if (null? command-words)
      command-stack 
      (cons command-words command-stack)))
