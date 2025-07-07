package zk.desktophelper;

import java.util.Arrays;

final class WaylandClipboardManager implements ClipboardManager {
  @Override
  public void setClipboard(String content) throws Exception {
    Utils.executeCommand(Arrays.asList(new String[] {"wl-copy", "-o"}), content);
  }

  @Override
  public String getClipboard() throws Exception {
    String content = Utils.executeCommand(Arrays.asList(new String[] {"wl-paste"}), null);
    // wl-paste may append a spurious newline.  Remove it
    if (content.endsWith("\n")) {
      content = content.substring(0, content.length() - 1);
    }
    return content;
  }
}
