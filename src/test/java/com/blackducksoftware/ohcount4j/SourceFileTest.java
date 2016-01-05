/**
 * Copyright 2016 Black Duck Software, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.blackducksoftware.ohcount4j;

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
    public void sourceBufferTestWithHeadAlreadyInit() throws IOException {
        String buffer = "This is a string";
        try (SourceFile source = new SourceFile("/foo/bar.baz", buffer)) {
            assertEquals("/foo/bar.baz", source.getPath());
            assertEquals("bar.baz", source.getName());
            assertEquals("baz", source.getExtension());
            assertEquals("This", source.head(4));
            assertEquals(buffer, source.head(-1));
            assertEquals("This ", source.head(5));
            for (int i = 0; i < buffer.length() + 10; i++) {
                int strIndex = (i < buffer.length()) ? i : buffer.length();
                String expectedValue = buffer.substring(0, strIndex);
                assertEquals(expectedValue, source.head(i));
            }
        }
    }

    @Test
    public void sourceBufferTestWithHeadAlreadyWithExactContent() throws IOException {
        String buffer = "This is a string";
        try (SourceFile source = new SourceFile("/foo/bar.baz", buffer)) {
            assertEquals("/foo/bar.baz", source.getPath());
            assertEquals("bar.baz", source.getName());
            assertEquals("baz", source.getExtension());
            assertEquals(buffer, source.head(buffer.length()));
            assertEquals(buffer, source.head(-1));
            assertEquals("This ", source.head(5));
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

    @Test
    public void sourceBufferTestWityNullSource() throws IOException {
        try (SourceFile source = new SourceFile("/foo/bar.baz", (String) null)) {
            assertEquals("", source.head(2));
        }
    }

    @Test
    public void sourceFileTest() throws IOException {
        String path = "src/test/java/com/blackducksoftware/ohcount4j/SourceFileTest.java";
        try (SourceFile source = new SourceFile(path)) {
            assertEquals(path, source.getPath());
            assertEquals("SourceFileTest.java", source.getName());
            assertEquals("java", source.getExtension());
            assertEquals("/**", source.head(3));
        }
    }

}
