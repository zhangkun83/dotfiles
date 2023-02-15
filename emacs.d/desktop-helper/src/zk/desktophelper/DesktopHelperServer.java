package zk.desktophelper;

import java.util.logging.Logger;

public class DesktopHelperServer {
  private static final Logger logger = Logger.getLogger(DesktopHelperServer.class.getName());

  public static void main(String[] args) throws Exception {
    Flags flags = new Flags(args);
    logger.info("DesktopHelperServer started with flags: " + flags);
    System.err.println(
        "##################################################################################\n"
        + "Local clients will work. To make remote clients work, use:\n"
        + "$ ssh <host> -R localhost:5035:localhost:5032 "
        + "-t ~/.emacs.d/bin/desktop-helper-proxy\n"
        + "to create an SSH tunnel and start a proxy.\n"
        + "The proxy will listen on the same port (5032) as the server, while SSH will tunnel\n"
        + "the server port 5032 on the local machine to port 5035 on the remote machine that the\n"
        + "proxy will connect to.\n"
        + "##################################################################################");
    int port = flags.getInt("port");
    new BlockingServer(port, new DesktopHelperServerWorker()).runServer();
  }
}
