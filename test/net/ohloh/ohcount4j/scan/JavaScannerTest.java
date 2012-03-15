package net.ohloh.ohcount4j.scan;

import org.testng.annotations.Test;

import net.ohloh.ohcount4j.scan.JavaScanner;
import static net.ohloh.ohcount4j.Entity.*;
import net.ohloh.ohcount4j.Language;

public class JavaScannerTest extends BaseScannerTest {

	@Test
	public void basic() {
		assertLine(new JavaScanner(), new Line(Language.JAVA, BLANK),   "\n");
		assertLine(new JavaScanner(), new Line(Language.JAVA, BLANK),   "     \n");
		assertLine(new JavaScanner(), new Line(Language.JAVA, BLANK),   "\t\n");
		assertLine(new JavaScanner(), new Line(Language.JAVA, CODE),    "import java.util.List;\n");
		assertLine(new JavaScanner(), new Line(Language.JAVA, COMMENT), "/* Block Comment */\n");
		assertLine(new JavaScanner(), new Line(Language.JAVA, COMMENT), "// Line comment\n");
		assertLine(new JavaScanner(), new Line(Language.JAVA, COMMENT), "//\n");
		assertLine(new JavaScanner(), new Line(Language.JAVA, CODE),    "import java.util.List; // with comment\n");
	}

	@Test
	public void eofHandling() {
		// Note lack of trailing \n in all cases below
		assertLine(new JavaScanner(), new Line(Language.JAVA, BLANK),   "     ");
		assertLine(new JavaScanner(), new Line(Language.JAVA, BLANK),   "\t");
		assertLine(new JavaScanner(), new Line(Language.JAVA, CODE),    "import java.util.List;");
		assertLine(new JavaScanner(), new Line(Language.JAVA, COMMENT), "/* Block Comment */");
		assertLine(new JavaScanner(), new Line(Language.JAVA, COMMENT), "// Line comment");
		assertLine(new JavaScanner(), new Line(Language.JAVA, COMMENT), "//");
		assertLine(new JavaScanner(), new Line(Language.JAVA, CODE),    "import java.util.List; // with comment");
	}

	@Test
	public void helloWorld() {
		String code
			= "/* Hello World\n"
			+ " * with multi-line comment */\n"
			+ "\n"
			+ "class HelloWorldApp {\n"
			+ "\tpublic static void main(String[] args) {\n"
			+ "\t\tSystem.out.println(\"Hello world!\");\n"
			+ "\t}\n"
			+ "}";

		Line[] expected = {
			new Line(Language.JAVA, COMMENT),
			new Line(Language.JAVA, COMMENT),
			new Line(Language.JAVA, BLANK),
			new Line(Language.JAVA, CODE),
			new Line(Language.JAVA, CODE),
			new Line(Language.JAVA, CODE),
			new Line(Language.JAVA, CODE),
			new Line(Language.JAVA, CODE)
		};
		assertLines(new JavaScanner(), expected, code);
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
		assertLines(new JavaScanner(), expected, code);
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
		assertLines(new JavaScanner(), expected, code);
	}
}
