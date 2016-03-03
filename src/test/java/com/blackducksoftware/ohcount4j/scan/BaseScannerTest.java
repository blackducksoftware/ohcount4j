/*
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

package com.blackducksoftware.ohcount4j.scan;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.Reader;
import java.io.StringReader;
import java.util.UUID;

import junit.framework.Assert;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.SourceFile;

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
