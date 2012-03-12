package net.ohloh.ohcount4j;

import static org.testng.AssertJUnit.assertEquals;

import java.io.IOException;

import net.ohloh.ohcount4j.io.SourceBuffer;
import net.ohloh.ohcount4j.io.SourceFile;

import org.testng.annotations.Test;

public class SourceTest {

	@Test
	public void sourceBufferTest() throws IOException {
		SourceBuffer source = new SourceBuffer("/foo/bar.baz", "This is a string");
		assertEquals("/foo/bar.baz", source.getPath());
		assertEquals("bar.baz", source.getName());
		assertEquals("baz", source.getExtension());
		assertEquals("This", source.head(4));
		assertEquals("This is a string", source.head(1000));
	}

	@Test
	public void sourceFileTest() throws IOException {
		String path = "test/net/ohloh/ohcount4j/SourceTest.java";
		SourceFile source = new SourceFile(path);
		assertEquals(path, source.getPath());
		assertEquals("SourceTest.java", source.getName());
		assertEquals("java", source.getExtension());
		assertEquals("package", source.head(7));
	}

}
