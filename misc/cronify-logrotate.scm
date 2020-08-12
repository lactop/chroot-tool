#!/usr/bin/guile
!#

(define (systemctl . cmd) (zero? (status:exit-val (apply system* "systemctl" cmd))))

(define (write-script . lines)
  (lambda ()
    (for-each (lambda (l) (display l) (newline)) lines)))

(define (install-daily-cron)
  (false-if-exception
    (let ((target "/etc/cron.daily/logrotate-system5"))
      (with-output-to-file
	target
	(write-script "#!/bin/sh"
		      "set -e"
		      "EXITVAL=$?"
		      "/usr/sbin/logrotate /etc/logrotate.conf || /usr/bin/logger -t logrotate \"ALERT exited abnormally with [$EXITVAL]\""
		      "exit $EXITVAL"))
      (chmod target #o755))))

(or (and (systemctl "stop" "logrotate.service")
	 (systemctl "stop" "logrotate.timer")
	 (systemctl "disable" "logrotate.service")
	 (systemctl "disable" "logrotate.timer")
	 (install-daily-cron)
	 (format #t "DONE~%"))
    (format #t "FAILURE~%"))
