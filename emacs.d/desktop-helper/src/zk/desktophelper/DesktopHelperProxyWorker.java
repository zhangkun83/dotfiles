package zk.desktophelper;

import static zk.desktophelper.Protocol.RESPONSE_HEADER_ERROR;
import static zk.desktophelper.Protocol.RESPONSE_HEADER_OK;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import zk.desktophelper.Protocol.Message;

final class DesktopHelperProxyWorker extends MessageWorker {
  private static final Logger logger = Logger.getLogger(DesktopHelperProxyWorker.class.getName());

  private final int serverPort;
  private final ScheduledExecutorService timerService =
      Executors.newSingleThreadScheduledExecutor();
  private volatile boolean connectedToServer = true;
  private volatile String fallbackClipboard;
  private volatile String fallbackUrlToOpen;
  private volatile boolean forceFallbackMode;
  private final ArrayBlockingQueue<Socket> socketToServer = new ArrayBlockingQueue<>(1);
  private final DesktopHelperHttpServer.Backend fallbackBackend =
      new DesktopHelperHttpServer.Backend() {
        @Override
        public String getClipboard() {
          return fallbackClipboard;
        }

        @Override
        public void setClipboard(String content) {
          fallbackClipboard = content;
        }

        @Override
        public String getUrlToOpen() {
          return fallbackUrlToOpen;
        }

        @Override
        public String getName() {
          return "Proxy Fallback Mode";
        }
      };
  private final DesktopHelperHttpServer httpServer;

  DesktopHelperProxyWorker(int serverPort, int httpPort, boolean httpOpenToNetwork)
      throws IOException {
    this.serverPort = serverPort;
    this.httpServer = new DesktopHelperHttpServer(httpPort, httpOpenToNetwork, fallbackBackend);
  }

  void start() {
    connect();
    httpServer.start();
    timerService.scheduleWithFixedDelay(this::ping, 0, 60, TimeUnit.SECONDS);
  }

  private void connect() {
    logger.fine("Connecting to server");
    try {
      Socket socket = new Socket("localhost", serverPort);
      logger.info("Connected to server at " + socket.getRemoteSocketAddress());
      socketToServer.add(socket);
      requestServer(
          new Message(
              "notify",
              "Testing, testing.\nThis is a proxy from "
              + InetAddress.getLocalHost().getHostName() + "."));
      connectedToServer = true;
    } catch (IOException e) {
      if (connectedToServer) {
        logger.warning("Failed to connect to server at port " + serverPort + ": " + e);
      }
      connectedToServer = false;
      scheduleReconnect();
    }
  }

  private void scheduleReconnect() {
    logger.fine("Waiting to reconnect to server");
    timerService.schedule(this::connect, 5, TimeUnit.SECONDS);
  }

  private void ping() {
    Message response = requestServer(new Message("ping", ""));
    boolean successful = RESPONSE_HEADER_OK.equals(response.header);
    if (successful != connectedToServer) {
      logger.info("Ping result: " + response);
      connectedToServer = successful;
      if (connectedToServer) {
        logger.info("Connection to server restored");
      } else {
        logger.info("Connection to server lost");
      }
    }
  }

  private Message requestServer(Message request) {
    Socket socket = null;
    try {
      socket = socketToServer.poll(10, TimeUnit.SECONDS);
    } catch (InterruptedException e) {
      if (connectedToServer) {
        logger.warning("Failed to get a connection to server: " + e);
      }
    }
    if (socket == null) {
      if (connectedToServer) {
        logger.warning("Proxy cannot get a connection to send " + request.header);
      }
      return new Message(RESPONSE_HEADER_ERROR, "Proxy cannot get a connection to server");
    } else {
      Message response;
      try {
        OutputStream outToServer = socket.getOutputStream();
        InputStream inFromServer = socket.getInputStream();
        Protocol.writeMessage(outToServer, request);
        logger.info("Sent " + request.header + " to server");
        response = Protocol.readMessage(inFromServer);
        logger.info("Response from server: " + response.header);
        socketToServer.add(socket);
        return response;
      } catch (IOException e) {
        logger.info("Proxy-server connection is broken: " + e);
        try {
          socket.close();
        } catch (IOException ignored) {}
        scheduleReconnect();
        return new Message(RESPONSE_HEADER_ERROR, "Proxy-server connection is broken");
      }
    }
  }

  @Override
  public void workOnConnection(MessageStream stream) throws IOException {
    while (true) {
      Message msg = stream.readMessage();
      logger.info("Received: " + msg.header);
      if (msg.header.equals("force-fallback")) {
        forceFallbackMode = msg.data.toLowerCase().equals("true");
        stream.writeMessage(new Message(
            RESPONSE_HEADER_OK,
            "Force fallback mode: " + forceFallbackMode));
        return;
      }
      stream.writeMessage(
          (connectedToServer && !forceFallbackMode)
          ? requestServer(msg) : fallbackHandleRequest(msg));
    }
  }

  private Message fallbackHandleRequest(Message request) {
    logger.info("Fallback handling " + request.header);
    if (request.header.equals("store-to-clipboard")) {
      fallbackClipboard = request.data;
      return new Message(
          RESPONSE_HEADER_OK,
          "Stored " + request.data.length() + " chars in proxy fallback clipboard");
    }
    if (request.header.equals("retrieve-from-clipboard")) {
      String clipboardCopy = fallbackClipboard;
      if (clipboardCopy == null) {
        return new Message(RESPONSE_HEADER_ERROR, "Proxy fallback clipboard is not set");
      }
      return new Message(RESPONSE_HEADER_OK, clipboardCopy);
    }
    if (request.header.equals("open-url")) {
      fallbackUrlToOpen = request.data;
      return new Message(RESPONSE_HEADER_OK, "URL stored in proxy fallback mode");
    }
    return new Message(
        RESPONSE_HEADER_ERROR, "Proxy fallback mode doesn't support " + request.header);
  }
}
