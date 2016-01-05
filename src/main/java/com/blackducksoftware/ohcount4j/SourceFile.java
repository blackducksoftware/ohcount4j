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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;

public class SourceFile implements AutoCloseable {

    private final String path;

    private final Reader reader;

    private char[] content;

    private boolean contentFromFile = true;

    public SourceFile(String path, Reader reader) {
        this.path = path;
        this.reader = reader;
        contentFromFile = false; // content not from file
    }

    public SourceFile(String path, String buffer) {
        this.path = path;
        reader = null;
        content = buffer != null ? buffer.toCharArray() : new char[0];
        contentFromFile = false; // content not from file
    }

    public SourceFile(String path) throws FileNotFoundException {
        this(new File(path));
    }

    public SourceFile(File file) throws FileNotFoundException {
        path = file.getPath();
        reader = new BufferedReader(new FileReader(file));
    }

    public String getPath() {
        return path;
    }

    public String getName() {
        return FilenameUtils.getName(path);
    }

    public String getExtension() {
        return FilenameUtils.getExtension(path);
    }

    public Reader getReader() {
        return reader;
    }

    /**
     * Returns if sourceFile is from FilePath i.e. the reader is initialized or not.
     * If it returns false then the contents were initialized by string buffer and it can be used directly.
     *
     * @return
     */
    public boolean isContentsFromFile() {
        return contentFromFile;
    }

    /**
     * Reads complete contents.
     *
     * @return
     * @throws IOException
     */
    public char[] getContents() throws IOException {
        return prepareContent(-1);
    }

    private char[] prepareContent(int maxLength) throws IOException {
        if (!contentFromFile) {
            // we got content from String buffer which already initialized the content, return it
            return content;
        }
        // Lazy load to avoid reading until required
        if (content == null) {
            if (maxLength == -1) {
                // read completely
                content = IOUtils.toCharArray(reader);
            } else {
                char[] buffer = new char[maxLength];
                int readLen = IOUtils.read(reader, buffer, 0, maxLength);
                // we don't know how much will we read, now copy the read length to contents
                content = new char[readLen];
                System.arraycopy(buffer, 0, content, 0, readLen);
            }
        } else if (maxLength == -1 || content.length < maxLength) {
            /*
             * When maxLength is -1
             * content is not null i.e. we did read something, and maxLength is -1 so read remaining.
             */
            int maxRemainingToRead = maxLength == -1 ? maxLength : (maxLength - content.length);
            readContentWithSpecifiedRemainingLength(maxRemainingToRead);
        }
        return content;
    }

    private void readContentWithSpecifiedRemainingLength(int remainingLength) throws IOException {
        char[] readRemaining;
        int readLen;
        if (remainingLength == -1) {
            readRemaining = IOUtils.toCharArray(reader);
            readLen = readRemaining.length;
        } else {
            // int maxRemainingToRead = maxLength - content.length;
            readRemaining = new char[remainingLength];
            // we don't know how much will we read, now copy the read length to contents
            readLen = IOUtils.read(reader, readRemaining, 0, remainingLength);
        }
        if (readLen != 0) {
            char[] newContents = new char[content.length + readLen];
            System.arraycopy(content, 0, newContents, 0, content.length);
            System.arraycopy(readRemaining, 0, newContents, content.length, readLen);
            content = newContents;
        }
    }

    // Regular expressions require CharSequence input
    public CharSequence getCharSequence() throws IOException {
        return java.nio.CharBuffer.wrap(getContents());
    }

    /**
     * Tries to read content as size specified. If maxLength is specified as -1 then it reads complete contents.
     */
    public String head(int maxLength) throws IOException {
        char[] contentsRead = prepareContent(maxLength);
        if (contentsRead.length <= maxLength || maxLength == -1) {
            return new String(contentsRead);
        } else {
            return new String(contentsRead, 0, maxLength);
        }
    }

    @Override
    public void close() throws IOException {
        if (reader != null) {
            reader.close();
        }
    }
}
