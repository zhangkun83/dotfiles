#!/bin/bash
set -e
python3 zk-py-shuangpin-generate-phrases-index.py

cat <<EOF > zk-py-shuangpin.el
;; GENERATED FILE.  DO NOT MODIFY.
;; Use zk-py-shuangpin-rebuild.sh to regenerate this file.
EOF

cat zk-py-shuangpin.el.base phrases-index.tmp >> zk-py-shuangpin.el

cat <<EOF >> zk-py-shuangpin.el
)

(provide 'zk-py-shuangpin)
EOF

echo "zk-py-shuangpin.el regenerated for pharses"
rm phrases-index.tmp
