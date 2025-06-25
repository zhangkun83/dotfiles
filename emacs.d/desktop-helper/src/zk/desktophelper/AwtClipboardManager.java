package zk.desktophelper;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;

final class AwtClipboardManager implements ClipboardManager {
  private final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();

  @Override
  public void setClipboard(String content) {
    clipboard.setContents(new StringSelection(content), null);
  }

  @Override
  public String getClipboard() throws Exception {
    return (String) clipboard.getData(DataFlavor.stringFlavor);
  }

}
