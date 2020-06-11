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

(defun duncan/org-html-publish-post-to-html (plist filename pub-dir)
  "Wraps org-html-publish-to-html.  Append post date as subtitle to PLIST.  FILENAME and PUB-DIR are passed."
  (let ((project (cons 'blog plist)))
    (plist-put plist :subtitle
               (format-time-string "%b %d, %Y" (org-publish-find-date filename project)))
    (duncan/org-html-publish-to-html plist filename pub-dir)))

(defun duncan/project-root ()
  "Thin (zero) wrapper over projectile to find project root."
  (projectile-project-root))

(defun duncan/project-relative-filename (filename)
  "Return the relative path of FILENAME to the project root."
  (file-relative-name filename (duncan/project-root)))

(defun duncan/org-html-publish-site-to-html (plist filename pub-dir)
  "Wraps org-html-publish-to-html.  Append css to hide title to PLIST and other front-page styles.  FILENAME and PUB-DIR are passed."
  (when (equal "index.org" (duncan/project-relative-filename filename))
    (plist-put plist :html-head-list
               (list
                (list "link"
                      (list "rel" "stylesheet" "href" (duncan/asset-relative-link-to "css/index.css" pub-dir t))))))
  (duncan/org-html-publish-to-html plist filename pub-dir))

                                        ; Project definition
(defvar spacemacs-cheatsheet--publish-project-alist
  (list
   (list "site"
         :base-directory "./"
         :include '("spacemacs_cheatsheet.org")
         :base-extension "org"
         :publishing-directory (expand-file-name "public" (projectile-project-root))
         :publishing-function 'org-html-publish-to-html
         :section-numbers nil
         :html-validation-link nil
         :html-head-include-scripts nil
         :html-head-include-default-style nil)))

                                        ; Our publishing definition
(defun spacemacs-cheatsheet-publish-all ()
  "Publish the blog to HTML."
  (interactive)
  (let ((make-backup-files nil)
        (org-publish-project-alist       spacemacs-cheatsheet--publish-project-alist)
        ;; deactivate cache as it does not take the publish.el file into account
        (user-full-name "Joey Bloom")
        (user-mail-address "15joeybloom@gmail.com")
        (org-publish-cache nil))
    (org-publish-all)))

(provide 'publish)
;;; publish.el ends here
