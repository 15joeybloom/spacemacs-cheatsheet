;; Borrowed heavily from
;; http://duncan.codes/posts/2019-09-03-migrating-from-jekyll-to-org/index.html

(require 'package)
(package-initialize)
(unless package-archive-contents
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-refresh-contents))
(dolist (pkg '(dash projectile org-plus-contrib htmlize))
  (unless (package-installed-p pkg)
    (package-install pkg)))

(require 'dash)
(require 'org)
(require 'ox-rss)
(require 'ox-publish)
(require 'projectile)

                                        ; Project definition
(defvar spacemacs-cheatsheet--publish-project-alist
  (list
   (list "site"
         :base-directory "./"
         :include '("index.org")
         :base-extension "org"
         :publishing-directory (expand-file-name "public" (projectile-project-root))
         :publishing-function 'org-html-publish-to-html
         :section-numbers nil
         :html-validation-link nil
         :html-head-include-scripts nil
         :html-head-include-default-style nil)))

                                        ; Our publishing definition
(defun spacemacs-cheatsheet-publish-all ()
  (interactive)
  (let ((make-backup-files nil)
        (org-publish-project-alist spacemacs-cheatsheet--publish-project-alist)
        ;; deactivate cache as it does not take the publish.el file into account
        (user-full-name "Joey Bloom")
        (user-mail-address "15joeybloom@gmail.com")
        (org-publish-cache nil))
    (org-publish-all)))

(provide 'publish)
;;; publish.el ends here
