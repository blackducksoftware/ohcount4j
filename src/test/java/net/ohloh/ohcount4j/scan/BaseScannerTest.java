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
package net.ohloh.ohcount4j.scan;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.Reader;
import java.util.UUID;

import net.ohloh.ohcount4j.SourceFile;

import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

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

    public void testScanWithSourceFile(SourceFile sourceFile, Class<?> exceptionClass) throws Exception {
        TempBaseScanner scanner = new TempBaseScanner();
        try {
            scanner.scan(sourceFile, new TempLineHandler());
        } catch (Exception e) {
            if (exceptionClass != null) {
                Assert.assertTrue(e.getCause().getClass().isAssignableFrom(exceptionClass),
                        "Expected " + exceptionClass + ", but got " + e);
            } else {
                Assert.fail("failed", e);
            }
        }
    }

    @DataProvider
    public Object[][] dataForScan() throws Exception {
        return new Object[][] {
                { new SourceFile(new File(UUID.randomUUID().toString())), NullPointerException.class },
                { new SourceFile("somenonexistingfile", "dd"), null }
        };
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
