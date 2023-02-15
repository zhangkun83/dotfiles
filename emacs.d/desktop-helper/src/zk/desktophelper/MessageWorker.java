package zk.desktophelper;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import zk.desktophelper.Protocol.Message;

abstract class MessageWorker implements BlockingServer.Worker {
  @Override
  public final void workOnConnection(InputStream in, OutputStream out) throws IOException {
    workOnConnection(new MessageStream(in, out));
  }

  abstract void workOnConnection(MessageStream stream) throws IOException;

  final class MessageStream {
    private final InputStream in;
    private final OutputStream out;

    MessageStream(InputStream in, OutputStream out) {
      this.in = in;
      this.out = out;
    }

    void writeMessage(Message msg) throws IOException {
      Protocol.writeMessage(out, msg);
    }

    void writeMessage(String header, String data) throws IOException {
      writeMessage(new Message(header, data));
    }

    Message readMessage() throws IOException {
      return Protocol.readMessage(in);
    }
  }
}
