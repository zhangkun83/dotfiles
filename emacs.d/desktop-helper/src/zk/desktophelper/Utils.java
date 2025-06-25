package zk.desktophelper;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;
import java.util.List;

final class Utils {
  static String executeCommand(List<String> command, String input) throws Exception {
    StringBuilder output = new StringBuilder();
    ProcessBuilder pb = new ProcessBuilder(command);
    pb.redirectErrorStream(true);
    Process process = pb.start();
    Charset systemCharset = Charset.defaultCharset();
    IOException inputError = null;
    if (input != null) {
      try (OutputStreamWriter writer =
          new OutputStreamWriter(process.getOutputStream(), systemCharset)) {
        writer.write(input);
      } catch (IOException ioe) {
        inputError = ioe;
      }
    }
    try (InputStreamReader reader =
        new InputStreamReader(process.getInputStream(), systemCharset)) {
      int c;
      while ((c = reader.read()) != -1) {
        output.append((char) c);
      }
    }
    int exitCode = process.waitFor();
    if (exitCode == 0) {
      return output.toString();
    } else {
      throw new Exception(
          "Command " + command + " failed with exit code " + exitCode
          + (inputError != null ? (", inputError: " + inputError.toString()) : "")
          + ", output: " + output.toString());
    }
  }
}
