module Actor where

type Actor a b = { a | process:a->b }