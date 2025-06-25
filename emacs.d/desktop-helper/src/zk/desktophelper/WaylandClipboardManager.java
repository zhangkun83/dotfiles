package zk.desktophelper;

import java.util.Arrays;

final class WaylandClipboardManager implements ClipboardManager {
  @Override
  public void setClipboard(String content) throws Exception {
    Utils.executeCommand(Arrays.asList(new String[] {"wl-copy", "-o"}), content);
  }

  @Override
  public String getClipboard() throws Exception {
    return Utils.executeCommand(Arrays.asList(new String[] {"wl-paste"}), null);
  }
}
