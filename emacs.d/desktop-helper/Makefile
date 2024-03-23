desktophelper.jar: $(shell find src -name '*.java')
	javac -d compile $^ && cp trayicon.png compile/zk/desktophelper/ && jar cf $@ -C compile .
run-server: desktophelper.jar
	java -Xmx60M -cp desktophelper.jar -Dawt.useSystemAAFontSettings=on -Dswing.aatext=true zk.desktophelper.DesktopHelperServer --port=5032
run-proxy: desktophelper.jar
	java -Xmx60M -cp desktophelper.jar zk.desktophelper.DesktopHelperProxy --server_port=5035 --proxy_port=5032 --http_port=5080
run-proxy-http-open-to-network: desktophelper.jar
	java -Xmx60M -cp desktophelper.jar zk.desktophelper.DesktopHelperProxy --server_port=5035 --proxy_port=5032 --http_port=5080 --http_open_to_network=true
clean:
	rm -r desktophelper.jar compile
