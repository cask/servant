(require 'f)
(require 'ansi-color)

(When "^I run servant \"\\([^\"]+\\)\"$"
  (lambda (args)
    (with-temp-buffer
      (let* ((args
              (s-split " " args))
             (bin-servant
              (f-join servant-test/root-path "bin" "servant"))
             (exit-code (apply 'call-process (append (list bin-servant nil t nil) args))))
        (let ((output (ansi-color-filter-apply (buffer-string))))
          (if (= exit-code 0)
              (setq servant-test/stdout output)
            (setq servant-test/stderr output)))))))

(Then "^I should see usage information$"
  (lambda ()
    (should (s-contains? "USAGE: servant [COMMAND] [OPTIONS]" servant-test/stdout))))

(Then "^I should see command output:$"
  (lambda (output)
    (should (s-contains? output servant-test/stdout))))

(Then "^I should not see command output:$"
  (lambda (output)
    (should-not (s-contains? output servant-test/stdout))))

(Then "^I should see command error:$"
  (lambda (output)
    (should (s-contains? output servant-test/stderr))))

(Then "^the directory \"\\([^\"]+\\)\" should exist$"
  (lambda (file)
    (should (f-dir? (f-expand file default-directory)))))
