package com.blackducksoftware.ohcount4j.scan;

import java.util.ArrayList;
import java.util.List;

public class TestLineHandler implements LineHandler {

    List<Line> lines;

    public TestLineHandler() {
        lines = new ArrayList<Line>();
    }

    @Override
    public void handleLine(Line line) {
        lines.add(line);
    }

    public List<Line> getLines() {
        return lines;
    }
}
