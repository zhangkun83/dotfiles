package zk.desktophelper;

import java.util.logging.Logger;

public class DesktopHelperProxy {
  private static final Logger logger = Logger.getLogger(DesktopHelperProxy.class.getName());

  public static void main(String[] args) {
    Flags flags = new Flags(args);
    logger.info("DesktopHelperProxy started with flags: " + flags);
    int proxyPort = flags.getInt("proxy_port");
    int serverPort = flags.getInt("server_port");
    DesktopHelperProxyWorker worker = new DesktopHelperProxyWorker(serverPort);
    worker.start();
    new BlockingServer(proxyPort, worker).start();
  }
}
