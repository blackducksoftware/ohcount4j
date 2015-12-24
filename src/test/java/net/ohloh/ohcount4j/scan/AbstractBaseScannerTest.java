package net.ohloh.ohcount4j.scan;

import static java.io.File.separator;
import static org.testng.Assert.fail;
import static org.testng.AssertJUnit.assertEquals;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import net.ohloh.ohcount4j.Language;
import net.ohloh.ohcount4j.OhcountException;
import net.ohloh.ohcount4j.SourceFile;

import org.testng.annotations.AfterMethod;

public abstract class AbstractBaseScannerTest {

    private static final String TEMP_SUFFIX = ".srcfile";

    private static final String TEMP_PREFIX = "____";

    private List<File> tempFiles = new ArrayList<File>();

    @AfterMethod
    public void afterTest() {
        for (File tf : tempFiles) {
            if (tf.exists() && !tf.delete()) {
                System.err.println("Could not delete temp file " + tf);
            }
        }
    }

    protected String getSourceCodePath(String fileName) {
        StringBuilder srcPath = new StringBuilder(System.getProperty("user.dir"));

        srcPath.append(separator).append("src").append(separator).append("test").
                append(separator).append("src-code").append(separator).append(fileName);

        return srcPath.toString();
    }

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

    private SourceFile createSourceFile(String code) throws IOException {
        File tempFile = createTempFile();
        SourceFile sourceFile = new SourceFile(tempFile);
        try (FileOutputStream outputStream = new FileOutputStream(tempFile)) {
            outputStream.write(code.getBytes());
        }
        return sourceFile;
    }

    protected File createTempFile() {
        try {
            File tempFile = File.createTempFile(TEMP_PREFIX, TEMP_SUFFIX);
            tempFiles.add(tempFile);
            return tempFile;
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

}
