(ns minesweeper.runner
    (:require [doo.runner :refer-macros [doo-tests]]
              [minesweeper.core-test]))

(doo-tests 'minesweeper.core-test)
