;;   THE FULL PATH TO THIS FILE SHOULD BE REFERRED TO IN THE ENVIRONMENT
;;   VARIABLE "I_BASH_CALLOUT", OR ELSE COPIED TO "$HOME/.bash_guile.scm",
;;   FOR EFFECT.


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



(let ((f (getenv "I_BASH_CALLOUT_2")))
  (cond  ((and f (access? f R_OK)) (load f))
         (else (let ((f (string-append (getenv "HOME")
                                       "/i-bash/remote-sender.scm")))
                 (cond ((access? f R_OK) (load f))
                       (else
                        (display "iBash: ERROR: no call-out-2.\n")
                        (exit 1)))))))
(use-modules (db core))
(display "GUILE INITIALIZATION COMPLETE.\n")


(define command-stack '())

(define (BASH:process-command-line command-words)
  (cond ((string=? (car command-words) "db")
         (set! command-stack (cons (cdr command-words) command-stack))
         #t)
        (else
         ;;  The process procedure comes from callout-2, loaded above.
         (let ((new-stack (process command-words command-stack)))
           (cond ((or (not new-stack)
                      (null? new-stack))
                  #f)
                 (else
                  (set! command-stack (reverse new-stack))
                  #t))))))

(define (BASH:next-command)
  (cond ((null? command-stack) #f)
        (else (let ((hold (car command-stack)))
                (set! command-stack (cdr command-stack))
                hold))))
