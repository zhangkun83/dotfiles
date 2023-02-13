package zk.desktophelper;

import static zk.desktophelper.Protocol.readMessage;
import static zk.desktophelper.Protocol.writeMessage;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import zk.desktophelper.Protocol.Message;

final class DesktopHelperProxyWorker implements BlockingServer.Worker {
  private static final Logger logger = Logger.getLogger(DesktopHelperProxyWorker.class.getName());

  private final int serverPort;
  private final ScheduledExecutorService timerService =
      Executors.newSingleThreadScheduledExecutor();
  private ArrayBlockingQueue<Socket> socketToServer = new ArrayBlockingQueue<>(1);

  DesktopHelperProxyWorker(int serverPort) {
    this.serverPort = serverPort;
  }

  void start() {
    connect();
    timerService.scheduleWithFixedDelay(this::ping, 0, 60, TimeUnit.SECONDS);
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
    logger.info("Waiting to reconnect to server");
    timerService.schedule(this::connect, 5, TimeUnit.SECONDS);
  }

  private void ping() {
    logger.info("Pinging server");
    Message response = requestServer(new Message("ping", ""));
    logger.info("Ping result: " + response);
  }

  private Message requestServer(Message request) {
    Socket socket = null;
    try {
      socket = socketToServer.poll(10, TimeUnit.SECONDS);
    } catch (InterruptedException e) {
      logger.warning("Failed to get a connection to server: " + e);
    }
    if (socket == null) {
      logger.warning("Proxy cannot get a connection to send " + request.header);
      return new Message("ERROR", "Proxy cannot get a connection to server");
    } else {
      Message response;
      try {
        OutputStream outToServer = socket.getOutputStream();
        InputStream inFromServer = socket.getInputStream();
        writeMessage(outToServer, request);
        logger.info("Sent " + request.header + " to server");
        response = readMessage(inFromServer);
        logger.info("Response from server: " + response.header);
        socketToServer.add(socket);
        return response;
      } catch (IOException e) {
        logger.info("Proxy-server connection is broken: " + e);
        scheduleReconnect();
        return new Message("ERROR", "Proxy-server connection is broken");
      }
    }
  }

  @Override
  public void workOnConnection(InputStream in, OutputStream out) throws IOException {
    while (true) {
      Message msg = readMessage(in);
      logger.info("Received: " + msg);
      writeMessage(out, requestServer(msg));
    }
  }
}
