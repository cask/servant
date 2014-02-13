(require 'f)

(When "^I run servant \"\\([^\"]+\\)\"$"
  (lambda (args)
    (with-temp-buffer
      (let* ((args
              (s-split " " args))
             (bin-servant
              (f-join servant-test/root-path "bin" "servant"))
             (exit-code (apply 'call-process (append (list bin-servant nil t nil) args))))
        (if (= exit-code 0)
            (setq servant-test/stdout (buffer-string))
          (setq servant-test/stderr (buffer-string)))))))

(Then "^I should see usage information$"
  (lambda ()
    (should (s-contains? "USAGE: servant [COMMAND] [OPTIONS]" servant-test/stdout))))
