package zk.desktophelper;

import java.util.Arrays;
import java.util.Map;
import java.util.stream.Collectors;

final class Flags {
  private final Map<String, String> data;

  Flags(String[] args) {
    data = parseFlags(args);
  }

  String getString(String name) {
    String value = data.get(name);
    if (value == null) {
      throw new IllegalArgumentException("Flag '" + name + "' unspecified");
    }
    return value;
  }

  String getString(String name, String defaultValue) {
    String value = data.get(name);
    if (value == null) {
      return defaultValue;
    }
    return value;
  }

  boolean getBoolean(String name, boolean defaultValue) {
    String value = data.get(name);
    if (value == null) {
      return false;
    }
    if ("yes".equalsIgnoreCase(value) || "true".equalsIgnoreCase(value)) {
      return true;
    }
    if ("no".equalsIgnoreCase(value) || "false".equalsIgnoreCase(value)) {
      return false;
    }
    throw new IllegalArgumentException("Invalid boolean value: " + value);
  }

  int getInt(String name) {
    return Integer.parseInt(getString(name));
  }

  @Override
  public String toString() {
    return data.toString();
  }

  private static Map<String, String> parseFlags(String[] args) {
    return Arrays.asList(args).stream()
        .map(arg -> {
              if (!arg.startsWith("--") || !arg.contains("=")) {
                throw new IllegalArgumentException("Malformed flag: " + arg);
              }
              return arg;
            })
        .collect(Collectors.toMap(
                arg -> {
                  int firstEqualPos = arg.indexOf("=");
                  return arg.substring(2, firstEqualPos);
                },
                arg -> {
                  int firstEqualPos = arg.indexOf("=");
                  return arg.substring(firstEqualPos + 1);
                }));
  }
}
