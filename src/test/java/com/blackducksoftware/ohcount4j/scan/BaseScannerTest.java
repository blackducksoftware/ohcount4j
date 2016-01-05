/*
 * Copyright (C) 2015 Black Duck Software Inc.
 * http://www.blackducksoftware.com/
 * All rights reserved.
 * 
 * This software is the confidential and proprietary information of
 * Black Duck Software ("Confidential Information"). You shall not
 * disclose such Confidential Information and shall use it only in
 * accordance with the terms of the license agreement you entered into
 * with Black Duck Software.
 */
package com.blackducksoftware.ohcount4j.scan;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.Reader;
import java.io.StringReader;
import java.util.UUID;

import junit.framework.Assert;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.SourceFile;

/**
 * @author mpujari
 *
 */
public class BaseScannerTest extends AbstractBaseScannerTest {

    @Test(expectedExceptions = FileNotFoundException.class)
    public void testSourceFileFnFe() throws Exception {
        try (SourceFile sf = new SourceFile(new File(UUID.randomUUID().toString()))) {
        }
    }

    @Test(expectedExceptions = FileNotFoundException.class)
    public void testScanWithSourceFile() throws Exception {
        try (SourceFile sf = new SourceFile(UUID.randomUUID().toString())) {
        }
    }

    @Test(expectedExceptions = NullPointerException.class)
    public void testScanWithSourceFilePathStrReaderNull() throws Exception {
        try (SourceFile sf = new SourceFile(UUID.randomUUID().toString(), (Reader) null)) {
            TempBaseScanner scanner = new TempBaseScanner();
            scanner.scan(sf, new TempLineHandler());
        }
    }

    @Test
    public void testScanWithSourceFilePathStrContentStr() throws Exception {
        try (SourceFile sf = new SourceFile(UUID.randomUUID().toString(), "Content buffer")) {
            TempBaseScanner scanner = new TempBaseScanner();
            scanner.scan(sf, new TempLineHandler());
        }
    }

    @Test
    public void testScanWithSourceFilePathExisting() throws Exception {
        try (SourceFile sf = new SourceFile(new File(getSourceCodePath("cstyle-1.c")))) {
            TempBaseScanner scanner = new TempBaseScanner();
            scanner.scan(sf, new TempLineHandler());
        }
    }

    @Test
    public void testScanWithSourceFilePathStringExisting() throws Exception {
        try (SourceFile sf = new SourceFile(new File(getSourceCodePath("cstyle-1.c")).getAbsolutePath())) {
            TempBaseScanner scanner = new TempBaseScanner();
            scanner.scan(sf, new TempLineHandler());
        }
    }

    @Test
    public void testScanWithSourceFilePathAndReader() throws Exception {
        try (SourceFile sf = new SourceFile("", new StringReader("Hello\n{dd}"))) {
            TempBaseScanner scanner = new TempBaseScanner();
            TempLineHandler handler = new TempLineHandler();
            scanner.scan(sf, handler);
            Assert.assertEquals(scanner.data, "Hello\n{dd}");
        }
    }

    @Test
    public void testScanWithSourceFilePathAndString() throws Exception {
        try (SourceFile sf = new SourceFile("", "Hello\n{dd}")) {
            TempBaseScanner scanner = new TempBaseScanner();
            TempLineHandler handler = new TempLineHandler();
            scanner.scan(sf, handler);
            Assert.assertEquals(scanner.data, "Hello\n{dd}");
        }
    }

    static class TempLineHandler implements LineHandler {
        int count;

        @Override
        public void handleLine(Line line) {
            count++;
        }
    }

    static class TempBaseScanner extends BaseScanner {
        String data;

        @Override
        public void doScan() {
            // do nothing
            data = new String(super.data);
        }
    }

}
