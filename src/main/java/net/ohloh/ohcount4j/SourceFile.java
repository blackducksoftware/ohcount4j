package net.ohloh.ohcount4j;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;

public class SourceFile implements AutoCloseable {

    private final String path;

    private final Reader reader;

    private char[] contents = null;

    public SourceFile(String path, Reader reader) {
        this.path = path;
        this.reader = reader;
    }

    public SourceFile(String path) throws FileNotFoundException {
        this.path = path;
        reader = new BufferedReader(new FileReader(path));
    }

    public SourceFile(File file) throws FileNotFoundException {
        path = file.getPath();
        reader = new BufferedReader(new FileReader(file));
    }

    public SourceFile(String path, String buffer) {
        this.path = path;
        reader = new StringReader(buffer);
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

    public char[] getContents() throws IOException {
        return getContents(-1);
    }

    private char[] getContents(int maxLength) throws IOException {
        // Lazy load to avoid reading until required
        if (contents == null) {
            if (maxLength == -1) {
                // read completely
                contents = IOUtils.toCharArray(reader);
            } else {
                char[] buffer = new char[maxLength];
                int readLen = IOUtils.read(reader, buffer, 0, maxLength);
                // we don't know how much will we read, now copy the read length to contents
                contents = new char[readLen];
                System.arraycopy(buffer, 0, contents, 0, readLen);
            }
        } else if (maxLength != -1 && contents.length < maxLength) {
            // we need to read remaining contents
            int maxRemainingToRead = maxLength - contents.length;
            char[] remainingContents = new char[maxRemainingToRead];
            int readLen = IOUtils.read(reader, remainingContents, 0, maxRemainingToRead);
            // we don't know how much will we read, now copy the read length to contents
            char[] newContents = new char[contents.length + readLen];
            System.arraycopy(contents, 0, newContents, 0, contents.length);
            System.arraycopy(remainingContents, 0, newContents, contents.length, readLen);
            contents = newContents;
        }
        return contents;
    }

    // Regular expressions require CharSequence input
    public CharSequence getCharSequence() throws IOException {
        return java.nio.CharBuffer.wrap(getContents());
    }

    // Ideally, head() should read only as much of the file as required.
    // For now, we simply read in the entire file and return only the first portion.
    public String head(int maxLength) throws IOException {
        char[] contentsRead = getContents(maxLength);
        if (contentsRead.length <= maxLength) {
            return new String(contentsRead);
        } else {
            return new String(contentsRead, 0, maxLength);
        }
    }

    @Override
    public void close() throws IOException {
        reader.close();
    }
}
