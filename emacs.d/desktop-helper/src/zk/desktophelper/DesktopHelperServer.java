package zk.desktophelper;

import java.util.logging.Logger;

public class DesktopHelperServer {
  private static final Logger logger = Logger.getLogger(DesktopHelperServer.class.getName());

  public static void main(String[] args) {
    Flags flags = new Flags(args);
    logger.info("DesktopHelperServer started with flags: " + flags);
    int port = flags.getInt("port");
    new BlockingServer(port, new DesktopHelperServerWorker()).start();
  }
}
