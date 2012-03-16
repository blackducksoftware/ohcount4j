package net.ohloh.ohcount4j.detect;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;

// Provides a minimal Java wrapper around the C magic library.
//
// To install libmagic:
//
//   OS X: `brew install libmagic`
//   Ubuntu: `apt-get install libmagic-dev`
//
public class Magic {

	public interface MagicLibrary extends Library {
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

	void open(int flags) {
		this.cookie = MagicLibrary.INSTANCE.magic_open(flags);
	}

	void open() {
		open(0);
	}

	void close() {
		MagicLibrary.INSTANCE.magic_close(this.cookie);
	}

	String error() {
		return MagicLibrary.INSTANCE.magic_error(this.cookie);
	}

	String buffer(String buf) {
		return MagicLibrary.INSTANCE.magic_buffer(this.cookie, buf, buf.length());
	}

	String file(String filename) {
		return MagicLibrary.INSTANCE.magic_file(this.cookie, filename);
	}

	// Load a magic number database from a file
	int load(String filename) {
		return MagicLibrary.INSTANCE.magic_load(this.cookie, filename);
	}

	// Load the default definitions database
	int load() {
		return load(null);
	}
}