package net.ohloh.ohcount4j;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.testng.AssertJUnit.assertEquals;

import java.io.CharArrayReader;
import java.io.IOException;
import java.io.Reader;

import net.ohloh.ohcount4j.io.Blob;
import net.ohloh.ohcount4j.io.InputStreamBlob;
import net.ohloh.ohcount4j.io.ReaderBlob;

import org.apache.commons.io.IOUtils;
import org.testng.annotations.Test;

public class BlobTest {

	@Test(expectedExceptions = { IllegalArgumentException.class })
	public void incompatibleReaderTest() {
		Reader r = mock(Reader.class);
		when(r.markSupported()).thenReturn(false);
		new ReaderBlob("name", r);
	}

	@Test
	public void charArrayReaderTest() throws IOException {
		String str = "This is a string";
		Reader r = new CharArrayReader(str.toCharArray());
		Blob source = new ReaderBlob("name", r);
		assertEquals(str.substring(0, 4), new String(source.peek(4)));
		assertEquals(str, new String(source.charContents()));
	}

	@Test
	public void inputStreamBlobTest() throws IOException {
		String name = "/data/input.c";
		InputStreamBlob blob = new InputStreamBlob(name,
				Ohcount.class.getResourceAsStream(name));
		assertEquals("//hello world", new String(blob.peek(13)));
		String contents = IOUtils.toString(Ohcount.class
				.getResourceAsStream(name));
		assertEquals(contents, new String(blob.charContents()));
	}
}
