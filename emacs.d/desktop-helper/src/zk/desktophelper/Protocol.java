package zk.desktophelper;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;

final class Protocol {
  private static final int SIZE_LENGTH = 4;
  private static final String CHARSET = "UTF-8";
  static final String RESPONSE_HEADER_OK = "OK";
  static final String RESPONSE_HEADER_ERROR = "ERROR";

  static final class Message {
    final String header;
    final String data;

    Message(String header, String data) {
      this.header = header;
      this.data = data;
    }

    @Override
    public String toString() {
      return "[header=" + header + ", data=" + data + "]";
    }
  }

  static Message readMessage(InputStream in) throws IOException {
    String header = readString(in);
    String data = readString(in);
    return new Message(header, data);
  }

  static String readString(InputStream in) throws IOException {
    byte[] sizeBytes = new byte[SIZE_LENGTH];
    readAll(in, sizeBytes);
    int size = ByteBuffer.wrap(sizeBytes).getInt();
    byte[] stringBytes = new byte[size];
    readAll(in, stringBytes);
    return new String(stringBytes, CHARSET);
  }

  static void writeMessage(OutputStream out, Message msg) throws IOException {
    writeString(out, msg.header);
    writeString(out, msg.data);
  }

  static void writeString(OutputStream out, String string) throws IOException {
    byte[] stringBytes = string.getBytes(CHARSET);
    byte[] sizeBytes = new byte[SIZE_LENGTH];
    ByteBuffer.wrap(sizeBytes).putInt(stringBytes.length);
    out.write(sizeBytes);
    out.write(stringBytes);
  }

  static void readAll(InputStream in, byte[] buffer) throws IOException {
    int pos = 0;
    while (pos < buffer.length) {
      int readCount = in.read(buffer, pos, buffer.length - pos);
      if (readCount == -1) {
        throw new IOException("Got EOF");
      }
      if (readCount <= 0) {
        throw new IOException("readCount=" + readCount);
      }
      pos += readCount;
    }
  }
}
