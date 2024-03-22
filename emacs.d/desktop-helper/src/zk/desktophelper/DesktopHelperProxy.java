package zk.desktophelper;

import java.util.logging.Logger;

public class DesktopHelperProxy {
  private static final Logger logger = Logger.getLogger(DesktopHelperProxy.class.getName());

  public static void main(String[] args) throws Exception {
    Flags flags = new Flags(args);
    logger.info("DesktopHelperProxy started with flags: " + flags);
    System.err.println(
        "##################################################################################\n"
        + "To connect to a real server on the desktop, use:\n"
        + "$ ssh <host> -4 -R 127.0.0.1:5035:127.0.0.1:5032 -o ExitOnForwardFailure=yes"
        + "-t ~/.emacs.d/bin/ssh-tunnel-stub.py\n"
        + "to create an SSH tunnel.\n"
        + "This proxy listens on the same port (5032) as the server, while SSH will tunnel\n"
        + "the server port 5032 on the local machine to port 5035 on the remote machine that the\n"
        + "proxy will connect to.\n"
        + "##################################################################################");

    int proxyPort = flags.getInt("proxy_port");
    int serverPort = flags.getInt("server_port");
    int httpPort = flags.getInt("http_port");
    logger.info("Starting HTTP server on port " + httpPort);
    DesktopHelperFallbackWorker fallbackWorker = new DesktopHelperFallbackWorker(httpPort);
    fallbackWorker.startHttpServer();
    DesktopHelperProxyWorker worker = new DesktopHelperProxyWorker(serverPort, fallbackWorker);
    worker.start();
    new BlockingServer(proxyPort, worker).runServer();
  }
}
