sed -i 's/;; Version: 2\.15/;; Version: 2\.16/g' *.el ui/*.el ui/buffers/*.el
sed -i 's/(define-package "org-social" "2\.15"/(define-package "org-social" "2\.16"/' org-social-pkg.el
