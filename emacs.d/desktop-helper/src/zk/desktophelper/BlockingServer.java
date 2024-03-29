package zk.desktophelper;

import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.logging.Level;
import java.util.logging.Logger;

final class BlockingServer {
  private static final Logger logger = Logger.getLogger(BlockingServer.class.getName());

  private final int port;
  private final Worker worker;
  private final Executor executor = Executors.newCachedThreadPool();

  BlockingServer(int port, Worker worker) {
    this.port = port;
    this.worker = worker;
  }

  void runServer() throws IOException {
    ServerSocket ss = new ServerSocket(port, 50, InetAddress.getByAddress(new byte[]{127, 0, 0, 1}));
    try {
      logger.info("BlockingServer listening on port " + port);
      while (true) {
        Socket s = ss.accept();
        executor.execute(() -> {
              logger.info("New client: " + s.getRemoteSocketAddress());
              try {
                InputStream in = s.getInputStream();
                OutputStream out = s.getOutputStream();
                worker.workOnConnection(in, out);
              } catch (IOException ioe) {
                logger.info(ioe.toString());
              } catch (RuntimeException e) {
                logger.log(Level.WARNING, "Exception thrown from Worker", e);
              }
              try {
                s.close();
              } catch (IOException e) {
                // ignored
              }
              logger.info("Client " + s.getRemoteSocketAddress() + " disconnected");
            });
      }
    } finally {
      try {
        ss.close();
      } catch (IOException e) {
        // ignored
      }
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
