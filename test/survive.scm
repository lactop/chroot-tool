(import (srfi srfi-19))

(define (daemonize process)
  (close-input-port (current-input-port))
  (close-output-port (current-error-port))
  (close-output-port (current-output-port))
  (port-for-each close-port) 
  (if (zero? (primitive-fork))
      (let ((p (getpid)))
        (setpgid p p)
        ; (with-output-to-file "/root/tmp/survivor.pid" (lambda () (write p) (newline)))
        (with-output-to-file "/root/tmp/survivor.log" process))
      (begin (sleep 5) (exit 0))))

(define signal-list '(SIGABRT
                      SIGIO
                      SIGPWR
                      SIGTTIN
                      SIGWINCH
                      SIGALRM
                      SIGFPE
                      SIGIOT
                      SIGQUIT
                      SIGTTOU
                      SIGXCPU
                      SIGBUS
                      SIGHUP
                      SIGRTMAX
                      SIGSYS
                      SIGURG
                      SIGXFSZ
                      SIGCHLD
                      SIGPIPE
                      SIGRTMIN
                      SIGTERM
                      SIGUSR1
                      SIGCLD
                      SIGILL
                      SIGPOLL
                      SIGSEGV
                      SIGTRAP
                      SIGUSR2
                      SIGCONT
                      SIGINT
                      SIGPROF
                      SIGSTKFLT
                      SIGTSTP
                      SIGVTALRM))

(define (heartbit)
  (for-each (lambda (s)
              (sigaction (eval s (current-module))
                         (lambda (i)
                           (format #t "Signal: ~a~%" s)
                           (force-output))))
            signal-list)

  (while #t
         (sleep 1)
         (format #t "Alive at ~a~%" (date->string (current-date)))
         (force-output)))

; (for-each (lambda (s) (format #t "~s(~s)~%" s (eval s (current-module))))
;           signal-list)
; (force-output)

(daemonize heartbit)
