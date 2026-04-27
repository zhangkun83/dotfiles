#!/bin/bash
set -e
python zk-py-shuangpin-generate-phrases-index.py
cat zk-py-shuangpin.el.base phrases-index.tmp > zk-py-shuangpin.el

cat <<EOF >> zk-py-shuangpin.el
)

(provide 'zk-py-shuangpin)
EOF

echo "zk-py-shuangpin.el regenerated for pharses"
rm phrases-index.tmp
