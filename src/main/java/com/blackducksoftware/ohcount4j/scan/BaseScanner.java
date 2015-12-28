package com.blackducksoftware.ohcount4j.scan;

import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Arrays;

import com.blackducksoftware.ohcount4j.Entity;
import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.SourceFile;

public abstract class BaseScanner implements Scanner {

    // 10000 line of code with 150 chars each (~1.5 MB file i.e 1572864 Bytes)
    private static final int BLOCK_SIZE = (int) (1024 * 1024 * 1.5); // 1.5 MB

    // Ragel variables.
    protected int[] stack = new int[32];

    protected char[] data;

    protected int cs;

    protected int top;

    protected int p;

    protected int eof;

    protected int pe = eof;

    protected int mark;

    protected int ts;

    protected int te;

    protected int act;

    protected int lineStart;

    protected LineHandler handler;

    protected Language defaultLanguage;

    protected Language language;

    protected ArrayList<Language> codeSeen = new ArrayList<Language>();

    protected ArrayList<Language> commentSeen = new ArrayList<Language>();

    // abstract method to be implemented by scanners
    public abstract void doScan();

    protected final void init() {
        pe = eof = data.length;
    }

    @Override
    public void setDefaultLanguage(Language language) {
        defaultLanguage = language;
    }

    @Override
    public Language getDefaultLanguage() {
        return defaultLanguage;
    }

    /**
     * When we have scan with sourceFile, we read data in blocks.
     * Ideally we read max block defined by BLOCK_SIZE (1.5MB) or the file size which ever is less.
     * 1.5MB came up with an assumption of a source file having 10000 line of code with 150 chars each has a size of
     * ~1.5 MB file i.e 1572864 Bytes.
     */
    @Override
    public final void scan(SourceFile source, LineHandler handler) throws IOException {
        Reader reader = source.getReader();
        try {
            long length = new File(source.getPath()).length();
            int buflen = (length > BLOCK_SIZE) ? BLOCK_SIZE : (int) length; // its OK we down-cast;
            if (buflen == 0) {
                // there can be a case where path is invalid, get content from reader if reader is initialized
                if (reader != null) {
                    buflen = BLOCK_SIZE;
                } else {
                    return; // there is nothing to read
                }
            }
            char[] cbuf = new char[buflen];
            int readLen;
            while ((readLen = reader.read(cbuf)) != -1) {
                char[] data0;
                if (readLen < buflen) {
                    // we have read less then cbuff length, copy till readLen
                    data0 = new char[readLen];
                    System.arraycopy(cbuf, 0, data0, 0, readLen);
                } else if (readLen == buflen && BLOCK_SIZE != buflen) {
                    // we don't need to copy anything, we got complete content of the file
                    data0 = cbuf;
                } else {
                    /*
                     * This block is executed when we have source file more then 1.5MB, so read the
                     * content to next newline (So that we can call scan(char[]).
                     */
                    char[] dataTillNewLine = readTillNewLine(reader);
                    data0 = new char[readLen + dataTillNewLine.length];
                    System.arraycopy(cbuf, 0, data0, 0, cbuf.length);
                    System.arraycopy(dataTillNewLine, 0, data0, cbuf.length, dataTillNewLine.length);
                }
                cs = top = p = eof = pe = mark = ts = te = act = lineStart = 0;
                scan(data0, handler);
            }
        } finally {
            if (reader != null) {
                reader.close();
            }
        }
    }

    private char[] readTillNewLine(Reader reader) throws IOException {
        int value;
        StringBuilder sb = new StringBuilder();
        while ((value = reader.read()) != -1) {
            if (value == '\n') {
                // break
                sb.append('\n');
                break;
            } else {
                sb.append(value);
            }
        }
        return sb.toString().toCharArray();
    }

    @Override
    public final void scan(String data, LineHandler handler) {
        scan(data.toCharArray(), handler);
    }

    @Override
    public final void scan(char[] data, LineHandler handler) {
        this.data = data;
        this.handler = handler;
        language = defaultLanguage;
        pe = eof = data.length;
        doScan();
        if (p != lineStart) { // EOF encountered without newline
            notifyNewline();
        }
    }

    // Called by Ragel machine when moving in or out of an embedded language.
    protected void setLanguage(Language lang) {
        language = lang;
    }

    protected void notifyCode() {
        if (!codeSeen.contains(language)) {
            codeSeen.add(language);
        }
    }

    protected void notifyComment() {
        if (!commentSeen.contains(language)) {
            commentSeen.add(language);
        }
    }

    // Given lists of all the code and comment langauages we've seen on this line,
    // choose one language and entity to represent the entire line.
    protected Line chooseLine() {
        // If we've seen any language besides the default language, use that one.
        for (Language l : codeSeen) {
            if (l != defaultLanguage) {
                return new Line(l, Entity.CODE);
            }
        }
        for (Language l : commentSeen) {
            if (l != defaultLanguage) {
                return new Line(l, Entity.COMMENT);
            }
        }
        // No embedded languages. Return the default language.
        if (codeSeen.size() > 0) {
            return new Line(codeSeen.get(0), Entity.CODE);
        } else if (commentSeen.size() > 0) {
            return new Line(commentSeen.get(0), Entity.COMMENT);
        } else {
            return new Line(language, Entity.BLANK);
        }
    }

    protected void notifyNewline() {
        Line line = chooseLine().appendContent(Arrays.copyOfRange(data, lineStart, p));

        if (handler != null) {
            handler.handleLine(line);
        }

        lineStart = p;
        codeSeen.clear();
        commentSeen.clear();
    }

    protected int match_begin_mark;

    protected int match_end_mark;

    protected int match_try_mark;

    protected void beginDefineMatch() {
        match_begin_mark = p;
    }

    protected void endDefineMatch() {
        match_end_mark = p;
    }

    protected void beginTryMatch() {
        match_try_mark = p;
    }

    protected boolean match() {
        if (match_begin_mark >= match_end_mark) {
            // No back reference has been defined
            return false;
        }

        int definition_length = match_end_mark - match_begin_mark;
        int try_length = p - match_try_mark + 1;

        if (definition_length != try_length) {
            return false;
        }

        for (int i = 0; i < try_length; i++) {
            if (data[match_try_mark + i] != data[match_begin_mark + i]) {
                return false;
            }
        }

        return true;
    }
}
