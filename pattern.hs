

test ls = (case ls of
              ~(_:ls') -> print ls'
              []       -> print 2
          )