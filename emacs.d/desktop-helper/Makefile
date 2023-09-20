desktophelper.jar: $(shell find src -name '*.java')
	javac -d compile $^ && cp trayicon.png compile/zk/desktophelper/ && jar cf $@ -C compile .
run-server: desktophelper.jar
	java -Xmx60M -cp desktophelper.jar zk.desktophelper.DesktopHelperServer --port=5032
run-proxy: desktophelper.jar
	java -Xmx60M -cp desktophelper.jar zk.desktophelper.DesktopHelperProxy --server_port=5035 --proxy_port=5032
clean:
	rm -r desktophelper.jar compile
