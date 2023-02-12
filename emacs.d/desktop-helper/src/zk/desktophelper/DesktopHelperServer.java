package zk.desktophelper;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.logging.Logger;

public class DesktopHelperServer {
  private static final Logger logger = Logger.getLogger(DesktopHelperServer.class.getName());

  private static final BlockingServer.Worker worker = new BlockingServer.Worker() {
      @Override
      public void workOnConnection(InputStream in, OutputStream out) throws IOException {
        byte[] inBuffer = new byte[1024];
        while (true) {
          int readSize = in.read(inBuffer);
          if (readSize <= 0) {
            return;
          }
          try {
            System.out.println("Received: " + new String(inBuffer, 0, readSize, "UTF-8"));
          } catch (Exception e) {
            // ignored
          }
        }
      }
    };
  
  public static void main(String[] args) {
    Flags flags = new Flags(args);
    logger.info("DesktopHelperServer started with flags: " + flags);
    int port = flags.getInt("port");
    new BlockingServer(port, worker).start();
  }
}
