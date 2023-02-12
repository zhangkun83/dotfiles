package zk.desktophelper;

import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.logging.Level;
import java.util.logging.Logger;

final class BlockingServer {
  private static final Logger logger = Logger.getLogger(BlockingServer.class.getName());

  private final int port;
  private final Worker worker;
  BlockingServer(int port, Worker worker) {
    this.port = port;
    this.worker = worker;
  }

  void start() {
    new Thread(this::acceptConnections).start();
  }

  private void acceptConnections() {
    try {
      ServerSocket ss = new ServerSocket(port);
      logger.info("BlockingServer listening on port " + port);
      while (true) {
        Socket s = ss.accept();
        new Thread(() -> {
              logger.info(
                  "New connection from " + s.getRemoteSocketAddress() + " dispatched to " + worker);
              try {
                InputStream in = s.getInputStream();
                OutputStream out = s.getOutputStream();
                worker.workOnConnection(in, out);
              } catch (IOException ioe) {
                logger.info(ioe.toString());
              } catch (RuntimeException e) {
                logger.log(Level.WARNING, "Exception thrown from Worker " + worker, e);
              }
              try {
                s.close();
              } catch (Exception e) {
                // ignored
              }
              logger.info("Connection from " + s.getRemoteSocketAddress() + " closed");
        }).start();
      }
    } catch (Exception e) {
      logger.log(Level.SEVERE, "ServerSocket broken", e);
    }
  }

  interface Worker {
    /**
     * Called from a thread dedicated to a connection and handles the I/O for that connection.  The
     * connection will be closed once this method returns.
     */
    void workOnConnection(InputStream in, OutputStream out) throws IOException;
  }
}
