package com.blackducksoftware.ohcount4j.scan;

import static com.blackducksoftware.ohcount4j.Entity.BLANK;
import static com.blackducksoftware.ohcount4j.Entity.CODE;
import static com.blackducksoftware.ohcount4j.Entity.COMMENT;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;

public class SqlScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.SQL, new Line(Language.SQL, BLANK), "\n");
        assertLine(Language.SQL, new Line(Language.SQL, BLANK), "     \n");
        assertLine(Language.SQL, new Line(Language.SQL, BLANK), "\t\n");
        assertLine(Language.SQL, new Line(Language.SQL, CODE), "SELECT * FROM test\n");
        assertLine(Language.SQL, new Line(Language.SQL, COMMENT), "/* Block Comment */\n");
        assertLine(Language.SQL, new Line(Language.SQL, COMMENT), "-- Line comment\n");
        assertLine(Language.SQL, new Line(Language.SQL, COMMENT), "--\n");
        assertLine(Language.SQL, new Line(Language.SQL, CODE), "CREATE TABLE 'test'( // with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.SQL, new Line(Language.SQL, BLANK), "     ");
        assertLine(Language.SQL, new Line(Language.SQL, BLANK), "\t");
        assertLine(Language.SQL, new Line(Language.SQL, CODE), "SELECT * FROM test");
        assertLine(Language.SQL, new Line(Language.SQL, COMMENT), "/* Block Comment */");
        assertLine(Language.SQL, new Line(Language.SQL, COMMENT), "-- Line comment");
        assertLine(Language.SQL, new Line(Language.SQL, COMMENT), "--");
        assertLine(Language.SQL, new Line(Language.SQL, CODE), "CREATE TABLE 'test'( // with comment");
    }

    @Test
    public void simpleTest() {
        String code = "/* Simple Sql Script Test\n"
                + "\t\n"
                + " multi-line comment */\n"
                + "\n"
                + "CREATE TABLE test (\n"
                + "\ttest_id int(10))\n"
                + "\n"
                + "SELECT * FROM test -- comment on code line\n";

        Line[] expected = {
                new Line(Language.SQL, COMMENT),
                new Line(Language.SQL, BLANK),
                new Line(Language.SQL, COMMENT),
                new Line(Language.SQL, BLANK),
                new Line(Language.SQL, CODE),
                new Line(Language.SQL, CODE),
                new Line(Language.SQL, BLANK),
                new Line(Language.SQL, CODE)
        };
        assertLines(Language.SQL, expected, code);
    }

    @Test
    public void unterminatedSqlBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "{\n\n\n";

        Line[] expected = {
                new Line(Language.SQL, COMMENT),
                new Line(Language.SQL, BLANK),
                new Line(Language.SQL, BLANK)
        };
        assertLines(Language.SQL, expected, code);
    }

}
