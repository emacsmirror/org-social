sed -i 's/;; Version: 2\.17/;; Version: 2\.18/g' *.el ui/*.el ui/buffers/*.el
sed -i 's/(define-package "org-social" "2\.17"/(define-package "org-social" "2\.18"/' org-social-pkg.el
