;; Sphinx docs build
(let ((default-directory "~/Projects/arlunio/docs"))
  (compile "make html"))

;; Sphinx docs build
(let ((default-directory "~/Projects/arlunio/blog"))
  (compile "python gallery.py -l --skip-failures"))
