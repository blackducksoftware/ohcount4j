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

import static org.testng.Assert.fail;
import static org.testng.AssertJUnit.assertEquals;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import com.blackducksoftware.ohcount4j.AbstractOhcount4jTest;
import com.blackducksoftware.ohcount4j.Language;
import com.blackducksoftware.ohcount4j.OhcountException;
import com.blackducksoftware.ohcount4j.SourceFile;

public abstract class AbstractBaseScannerTest extends AbstractOhcount4jTest {

    protected void assertLine(Language language, Line expected, String code) {
        assertLines(language, new Line[] { expected }, code);
    }

    /**
     * Assert lines as well as creates temp file with the code and validates with assertWithSourceFileLines calls
     *
     * @param language
     * @param expected
     * @param code
     */
    protected void assertLines(Language language, Line[] expected, String code) {
        TestLineHandler h = new TestLineHandler();

        try {
            language.makeScanner().scan(code, h);
        } catch (OhcountException e) {
            fail("Could not instantiate scanner", e.getCause());
        }

        assertEquals(expected.length, h.getLines().size());

        for (int i = 0; i < expected.length; i++) {
            Line line = h.getLines().get(i);
            String msg = String.format("at line %1$d: %2$s", i + 1, line.getContent());
            assertEquals(msg, expected[i].language, line.language);
            assertEquals(msg, expected[i].entity, line.entity);
        }
        // validate the source file by creating one
        assertWithSourceFileLines(language, expected, code);
    }

    protected void assertWithSourceFileLines(Language language, Line[] expected, String code) {
        SourceFile sourceFile;
        try {
            sourceFile = createSourceFile(code);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        assertLines(language, expected, sourceFile);
    }

    private SourceFile createSourceFile(String code) throws IOException {
        File tempFile = createTempFile();
        SourceFile sourceFile = new SourceFile(tempFile);
        try (FileOutputStream outputStream = new FileOutputStream(tempFile)) {
            outputStream.write(code.getBytes());
        }
        return sourceFile;
    }

    protected void assertLines(Language language, Line[] expected, SourceFile sourceFile) {
        TestLineHandler h = new TestLineHandler();
        try {
            language.makeScanner().scan(sourceFile, h);
        } catch (OhcountException e) {
            fail("Could not instantiate scanner", e.getCause());
        } catch (IOException e) {
            fail("IOException during scan", e);
        }

        assertEquals(expected.length, h.getLines().size());

        for (int i = 0; i < expected.length; i++) {
            Line line = h.getLines().get(i);
            String msg = String.format("at line %1$d: %2$s", i + 1, line.getContent());
            assertEquals(msg, expected[i].language, line.language);
            assertEquals(msg, expected[i].entity, line.entity);
        }
    }

}
