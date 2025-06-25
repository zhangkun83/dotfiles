package zk.desktophelper;

interface ClipboardManager {
  void setClipboard(String content) throws Exception;
  String getClipboard() throws Exception;
}
