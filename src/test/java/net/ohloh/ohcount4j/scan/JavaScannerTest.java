package net.ohloh.ohcount4j.scan;

import static net.ohloh.ohcount4j.Entity.BLANK;
import static net.ohloh.ohcount4j.Entity.CODE;
import static net.ohloh.ohcount4j.Entity.COMMENT;
import net.ohloh.ohcount4j.Language;

import org.testng.annotations.Test;

public class JavaScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.JAVA, new Line(Language.JAVA, BLANK), "\n");
        assertLine(Language.JAVA, new Line(Language.JAVA, BLANK), "     \n");
        assertLine(Language.JAVA, new Line(Language.JAVA, BLANK), "\t\n");
        assertLine(Language.JAVA, new Line(Language.JAVA, CODE), "import java.util.List;\n");
        assertLine(Language.JAVA, new Line(Language.JAVA, COMMENT), "/* Block Comment */\n");
        assertLine(Language.JAVA, new Line(Language.JAVA, COMMENT), "/** Block Comment **/\n");
        assertLine(Language.JAVA, new Line(Language.JAVA, COMMENT), "// Line comment\n");
        assertLine(Language.JAVA, new Line(Language.JAVA, COMMENT), "//\n");
        assertLine(Language.JAVA, new Line(Language.JAVA, CODE), "import java.util.List; // with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.JAVA, new Line(Language.JAVA, BLANK), "     ");
        assertLine(Language.JAVA, new Line(Language.JAVA, BLANK), "\t");
        assertLine(Language.JAVA, new Line(Language.JAVA, CODE), "import java.util.List;");
        assertLine(Language.JAVA, new Line(Language.JAVA, COMMENT), "/* Block Comment */");
        assertLine(Language.JAVA, new Line(Language.JAVA, COMMENT), "/** Block Comment **/");
        assertLine(Language.JAVA, new Line(Language.JAVA, COMMENT), "// Line comment");
        assertLine(Language.JAVA, new Line(Language.JAVA, COMMENT), "//");
        assertLine(Language.JAVA, new Line(Language.JAVA, CODE), "import java.util.List; // with comment");
    }

    @Test
    public void helloWorld() {
        String code = "/* Hello World\n"
                + " * with multi-line comment */\n"
                + "\n"
                + "class HelloWorldApp {\n"
                + "\tpublic static void main(String[] args) {\n"
                + "\t\tSystem.out.println(\"Hello world!\");\n"
                + "/** comment with 2 *s **/\n"
                + "\t}\n"
                + "}";

        Line[] expected = {
                new Line(Language.JAVA, COMMENT),
                new Line(Language.JAVA, COMMENT),
                new Line(Language.JAVA, BLANK),
                new Line(Language.JAVA, CODE),
                new Line(Language.JAVA, CODE),
                new Line(Language.JAVA, CODE),
                new Line(Language.JAVA, COMMENT),
                new Line(Language.JAVA, CODE),
                new Line(Language.JAVA, CODE)
        };
        assertLines(Language.JAVA, expected, code);
    }

    @Test
    public void unterminatedMultilineStringCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "'\nA\n\n";

        Line[] expected = {
                new Line(Language.JAVA, CODE),
                new Line(Language.JAVA, CODE),
                new Line(Language.JAVA, BLANK)
        };
        assertLines(Language.JAVA, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "/*\n\n\n";

        Line[] expected = {
                new Line(Language.JAVA, COMMENT),
                new Line(Language.JAVA, BLANK),
                new Line(Language.JAVA, BLANK)
        };
        assertLines(Language.JAVA, expected, code);
    }
}
