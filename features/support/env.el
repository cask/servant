(require 'f)

(defvar servant-test/support-path
  (f-dirname (f-this-file)))

(defvar servant-test/features-path
  (f-parent servant-test/support-path))

(defvar servant-test/root-path
  (f-parent servant-test/features-path))

(defvar servant-test/sandbox-path
  (f-expand "sandbox" servant-test/features-path))

(defvar servant-test/stdout nil)
(defvar servant-test/stderr nil)

(add-to-list 'load-path servant-test/root-path)

(require 'servant)
(require 'espuds)
(require 'ert)

(Before
 (setq servant-test/stdout "")
 (setq servant-test/stderr "")

 (when (f-dir? servant-test/sandbox-path)
   (f-delete servant-test/sandbox-path 'force))
 (f-mkdir servant-test/sandbox-path "tmp")
 (setq default-directory (f-slash servant-test/sandbox-path)))
