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
import java.util.UUID;

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

    @Test
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

    static class TempLineHandler implements LineHandler {
        @Override
        public void handleLine(Line line) {
            // do nothing
        }
    }

    static class TempBaseScanner extends BaseScanner {
        @Override
        public void doScan() {
            // do nothing
        }
    }

}