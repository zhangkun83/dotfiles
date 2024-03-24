package zk.desktophelper;

import java.util.logging.Logger;

public class DesktopHelperServer {
  private static final Logger logger = Logger.getLogger(DesktopHelperServer.class.getName());

  public static void main(String[] args) throws Exception {
    Flags flags = new Flags(args);
    logger.info("DesktopHelperServer started with flags: " + flags);
    int port = flags.getInt("port");
    int httpPort = flags.getInt("http_port");
    boolean httpOpenToNetwork = flags.getBoolean("http_open_to_network", false);
    boolean useSystemNotifications =
        "true".equals(flags.getString("use_system_notifications", "false"));
    new BlockingServer(
        port,
        new DesktopHelperServerWorker(
            useSystemNotifications, httpPort, httpOpenToNetwork))
        .runServer();
  }
}
