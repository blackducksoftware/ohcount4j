package net.ohloh.ohcount4j.io;

import java.io.IOException;
import java.io.Reader;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;

public class ReaderBlob implements Blob {

	private final String path;
	private final Reader reader;

	public ReaderBlob(String path, Reader reader) {
		this.path = path;
		this.reader = reader;
		if (!this.reader.markSupported()) {
			// FIXME - Remove this restriction.
			throw new IllegalArgumentException(
					"Reader for inputstream doesn't support mark()");
		}
	}

	@Override
	public String getPath() {
		return path;
	}

	@Override
	public String getName() {
		return FilenameUtils.getName(path);
	}

	@Override
	public String getExtension() {
		return FilenameUtils.getExtension(path);
	}

	@Override
	public char[] charContents() throws IOException {
		return IOUtils.toCharArray(reader);
	}

	@Override
	public char[] peek(int length) throws IOException {
		char[] buf = new char[length];
		reader.mark(length);
		int nbytes = reader.read(buf, 0, length);
		if (nbytes == -1) {
			throw new IOException("End of stream has reacherd.");
		}
		reader.reset();
		return buf;
	}
}
