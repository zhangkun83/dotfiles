package zk.desktophelper;

import static zk.desktophelper.Protocol.readMessage;
import static zk.desktophelper.Protocol.writeMessage;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import zk.desktophelper.Protocol.Message;

final class DesktopHelperProxyWorker implements BlockingServer.Worker {
  private static final Logger logger = Logger.getLogger(DesktopHelperProxyWorker.class.getName());

  private final int serverPort;
  private ArrayBlockingQueue<Socket> socketToServer = new ArrayBlockingQueue<>(1);

  DesktopHelperProxyWorker(int serverPort) {
    this.serverPort = serverPort;
  }

  void start() {
    connect();
  }

  private void connect() {
    logger.info("Connecting to server");
    try {
      Socket socket = new Socket("localhost", serverPort);
      logger.info("Connected to server at " + socket.getRemoteSocketAddress());
      socketToServer.add(socket);
    } catch (IOException e) {
      logger.warning("Failed to connect to server at port " + serverPort + ": " + e);
      scheduleReconnect();
    }
  }

  private void scheduleReconnect() {
    new Thread(() -> {
          logger.info("Waiting to reconnect to server");
          try {
            Thread.sleep(5000);
          } catch (Exception ignored) {}
          connect();
    }).start();
  }

  @Override
  public void workOnConnection(InputStream in, OutputStream out) throws IOException {
    while (true) {
      Message msg = readMessage(in);
      logger.info("Received: " + msg);
      Socket socket = null;
      try {
        socketToServer.poll(10, TimeUnit.SECONDS);
      } catch (InterruptedException e) {
        logger.warning("Failed to get connect to server: " + e);
      }
      if (socket == null) {
        writeMessage(out, new Message("ERROR", "Proxy cannot get a connection to server"));
      } else {
        Message response;
        try {
          OutputStream outToServer = socket.getOutputStream();
          InputStream inFromServer = socket.getInputStream();
          writeMessage(outToServer, msg);
          logger.info("Forwarded request to server");
          response = readMessage(inFromServer);
          logger.info("Response from server: " + response);
          socketToServer.add(socket);
        } catch (IOException e) {
          writeMessage(out, new Message("ERROR", "Proxy-server connection is broken"));
          logger.info("Proxy-server connection is broken: " + e);
          scheduleReconnect();
          continue;
        }
        writeMessage(out, response);
        logger.info("Forwarded response to client");
      }
    }
  }
}
