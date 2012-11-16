package net.ohloh.ohcount4j.detect;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;

// Provides a minimal Java wrapper around the C magic library.
//
// To install libmagic:
//
// OS X: `brew install libmagic`
// Ubuntu: `apt-get install libmagic-dev`
//
public class Magic {

    private interface MagicLibrary extends Library {
        MagicLibrary INSTANCE =
                (MagicLibrary) Native.loadLibrary("magic", MagicLibrary.class);

        Pointer magic_open(int flags);

        void magic_close(Pointer cookie);

        String magic_error(Pointer cookie);

        String magic_buffer(Pointer cookie, String buffer, int length);

        String magic_file(Pointer cookie, String filename);

        int magic_load(Pointer cookie, String filename);
    }

    protected Pointer cookie;

    public void open(int flags) {
        cookie = MagicLibrary.INSTANCE.magic_open(flags);
    }

    public void open() {
        open(0);
    }

    public void close() {
        MagicLibrary.INSTANCE.magic_close(cookie);
    }

    public String error() {
        return MagicLibrary.INSTANCE.magic_error(cookie);
    }

    public String buffer(String buf) {
        return MagicLibrary.INSTANCE.magic_buffer(cookie, buf, buf.length());
    }

    public String file(String filename) {
        return MagicLibrary.INSTANCE.magic_file(cookie, filename);
    }

    // Load a magic number database from a file
    public int load(String filename) {
        return MagicLibrary.INSTANCE.magic_load(cookie, filename);
    }

    // Load the default definitions database
    public int load() {
        return load(null);
    }

}
