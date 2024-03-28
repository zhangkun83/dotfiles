package zk.desktophelper;

import java.util.logging.Logger;

public class DesktopHelperFileSystemServer {
  private static final Logger logger =
      Logger.getLogger(DesktopHelperFileSystemServer.class.getName());

  public static void main(String[] args) throws Exception {
    Flags flags = new Flags(args);
    logger.info("DesktopHelperFileSystemServer started with flags: " + flags);
    int port = flags.getInt("port");
    new BlockingServer(port, new DesktopHelperFileSystemServerWorker()) .runServer();
  }
}
