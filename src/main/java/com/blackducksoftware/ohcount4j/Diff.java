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
package com.blackducksoftware.ohcount4j;

/**
 * @author mpujari
 *
 */
public class Diff {

    private int codeLinesAdded;

    private int codeLinesRemoved;

    private int commentLinesAdded;

    private int commentLinesRemoved;

    private int blankLinesAdded;

    private int blankLinesRemoved;

    public Diff(int codeLinesAdded, int codeLinesRemoved, int commentLinesAdded, int commentLinesRemoved,
            int blankLinesAdded, int blankLinesRemoved) {
        this.codeLinesAdded = codeLinesAdded;
        this.codeLinesRemoved = codeLinesRemoved;
        this.commentLinesAdded = commentLinesAdded;
        this.commentLinesRemoved = commentLinesRemoved;
        this.blankLinesAdded = blankLinesAdded;
        this.blankLinesRemoved = blankLinesRemoved;
    }

    public int getCodeLinesAdded() {
        return codeLinesAdded;
    }

    public int getCodeLinesRemoved() {
        return codeLinesRemoved;
    }

    public int getCommentLinesAdded() {
        return commentLinesAdded;
    }

    public int getCommentLinesRemoved() {
        return commentLinesRemoved;
    }

    public int getBlankLinesAdded() {
        return blankLinesAdded;
    }

    public int getBlankLinesRemoved() {
        return blankLinesRemoved;
    }

    @Override
    public String toString() {
        return String.format("%1$d\t%2$d\t%3$d\t%4$d\t%5$d\t%6$d", codeLinesAdded, codeLinesRemoved, commentLinesAdded,
                commentLinesRemoved, blankLinesAdded, blankLinesRemoved);
    }

}
