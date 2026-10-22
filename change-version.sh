sed -i 's/;; Version: 2\.16/;; Version: 2\.17/g' *.el ui/*.el ui/buffers/*.el
sed -i 's/(define-package "org-social" "2\.16"/(define-package "org-social" "2\.17"/' org-social-pkg.el
