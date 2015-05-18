package net.ohloh.ohcount4j;

import static org.testng.AssertJUnit.assertEquals;

import java.io.IOException;

import org.testng.annotations.Test;

public class SourceFileTest {

    @Test
    public void sourceBufferTest() throws IOException {
        String buffer = "This is a string";
        try (SourceFile source = new SourceFile("/foo/bar.baz", buffer)) {
            assertEquals("/foo/bar.baz", source.getPath());
            assertEquals("bar.baz", source.getName());
            assertEquals("baz", source.getExtension());
            assertEquals("This", source.head(4));
            assertEquals(buffer, source.head(1000));
            for (int i = 0; i < buffer.length() + 10; i++) {
                int strIndex = (i < buffer.length()) ? i : buffer.length();
                String expectedValue = buffer.substring(0, strIndex);
                assertEquals(expectedValue, source.head(i));
            }
        }
    }

    @Test
    public void sourceBufferTestReadMaxFirst() throws IOException {
        String buffer = "This is a string";
        try (SourceFile source = new SourceFile("/foo/bar.baz", buffer)) {
            assertEquals("/foo/bar.baz", source.getPath());
            assertEquals("bar.baz", source.getName());
            assertEquals("baz", source.getExtension());
            assertEquals(buffer, source.head(1000));
            for (int i = 0; i < buffer.length() + 10; i++) {
                int strIndex = (i < buffer.length()) ? i : buffer.length();
                String expectedValue = buffer.substring(0, strIndex);
                assertEquals(expectedValue, source.head(i));
            }
        }
    }

    @Test
    public void sourceBufferTestReadMinFirst() throws IOException {
        String buffer = "This is a string";
        try (SourceFile source = new SourceFile("/foo/bar.baz", buffer)) {
            assertEquals("/foo/bar.baz", source.getPath());
            assertEquals("bar.baz", source.getName());
            assertEquals("baz", source.getExtension());
            assertEquals("", source.head(0));
            for (int i = buffer.length() + 10; i >= 0; i--) {
                int strIndex = (i < buffer.length()) ? i : buffer.length();
                String expectedValue = buffer.substring(0, strIndex);
                assertEquals(expectedValue, source.head(i));
            }
        }
    }

    @Test
    public void sourceBufferTestWityEmptySource() throws IOException {
        try (SourceFile source = new SourceFile("/foo/bar.baz", "")) {
            assertEquals("/foo/bar.baz", source.getPath());
            assertEquals("bar.baz", source.getName());
            assertEquals("baz", source.getExtension());
            assertEquals("", source.head(4));
            assertEquals("", source.head(1000));
            assertEquals("", source.head(2));
        }
    }

    @Test(expectedExceptions = NullPointerException.class)
    public void sourceBufferTestWityNullSource() throws IOException {
        try (SourceFile source = new SourceFile("/foo/bar.baz", (String) null)) {
            assertEquals("", source.head(2));
        }
    }

    @Test
    public void sourceFileTest() throws IOException {
        String path = "src/test/java/net/ohloh/ohcount4j/SourceFileTest.java";
        try (SourceFile source = new SourceFile(path)) {
            assertEquals(path, source.getPath());
            assertEquals("SourceFileTest.java", source.getName());
            assertEquals("java", source.getExtension());
            assertEquals("package", source.head(7));
        }
    }

}
