package net.ohloh.ohcount4j;

import static org.testng.AssertJUnit.assertEquals;

import java.io.IOException;

import net.ohloh.ohcount4j.SourceFile;

import org.testng.annotations.Test;

public class SourceFileTest {

	@Test
	public void sourceBufferTest() throws IOException {
		SourceFile source = new SourceFile("/foo/bar.baz", "This is a string");
		assertEquals("/foo/bar.baz", source.getPath());
		assertEquals("bar.baz", source.getName());
		assertEquals("baz", source.getExtension());
		assertEquals("This", source.head(4));
		assertEquals("This is a string", source.head(1000));
	}

	@Test
	public void sourceFileTest() throws IOException {
		String path = "test/net/ohloh/ohcount4j/SourceFileTest.java";
		SourceFile source = new SourceFile(path);
		assertEquals(path, source.getPath());
		assertEquals("SourceFileTest.java", source.getName());
		assertEquals("java", source.getExtension());
		assertEquals("package", source.head(7));
	}

}
