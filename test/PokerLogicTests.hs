

-- Test that the function correctly identifies a royal flush:
-- Input: ["Ah", "Kh", "Qh", "Jh", "10h"]
-- Expected output: "Royal Flush"
-- Test that the function correctly identifies a straight flush:
-- Input: ["2d", "3d", "4d", "5d", "6d"]
-- Expected output: "Straight Flush"
-- Test that the function correctly identifies four of a kind:
-- Input: ["As", "Ac", "Ad", "Ah", "2h"]
-- Expected output: "Four of a Kind"
-- Test that the function correctly identifies a full house:
-- Input: ["Ks", "Kc", "Kd", "Qh", "Qs"]
-- Expected output: "Full House"
-- Test that the function correctly identifies a flush:
-- Input: ["2s", "7s", "9s", "Js", "Qs"]
-- Expected output: "Flush"
-- Test that the function correctly identifies a straight:
-- Input: ["2s", "3c", "4d", "5h", "6s"]
-- Expected output: "Straight"
-- Test that the function correctly identifies three of a kind:
-- Input: ["7h", "7c", "7d", "3s", "10c"]
-- Expected output: "Three of a Kind"
-- Test that the function correctly identifies two pair:
-- Input: ["9h", "9c", "10d", "10s", "4d"]
-- Expected output: "Two Pair"
-- Test that the function correctly identifies one pair:
-- Input: ["6h", "6c", "3d", "5s", "10h"]
-- Expected output: "One Pair"
-- Test that the function correctly identifies high card:
-- Input: ["4h", "7c", "8d", "10s", "Qh"]
-- Expected output: "High Card"

-- Test that two identical high card hands are equal:
-- Input: ["2h", "4d", "7c", "9s", "Jh"], ["2d", "4h", "7s", "9c", "Jd"]
-- Expected output: "Equal"
-- Test that two identical one pair hands are equal:
-- Input: ["3h", "3c", "5d", "9s", "Jh"], ["3d", "3s", "5h", "9c", "Jd"]
-- Expected output: "Equal"
-- Test that two identical two pair hands are equal:
-- Input: ["3h", "3c", "5d", "5s", "Jh"], ["3d", "3s", "5h", "5c", "Jd"]
-- Expected output: "Equal"
-- Test that two identical three of a kind hands are equal:
-- Input: ["3h", "3c", "3d", "5s", "Jh"], ["3s", "3d", "3c", "5h", "Jd"]
-- Expected output: "Equal"
-- Test that two identical straight hands are equal:
-- Input: ["2h", "3c", "4d", "5s", "6h"], ["2d", "3s", "4h", "5c", "6d"]
-- Expected output: "Equal"
-- Test that two identical flush hands are equal:
-- Input: ["2h", "4h", "6h", "8h", "10h"], ["2d", "4d", "6d", "8d", "10d"]
-- Expected output: "Equal"
-- Test that two identical full house hands are equal:
-- Input: ["3h", "3c", "3d", "Jh", "Js"], ["3s", "3d", "3c", "Jd", "Jc"]
-- Expected output: "Equal"
-- Test that two identical four of a kind hands are equal:
-- Input: ["3h", "3c", "3d", "3s", "Jh"], ["3d", "3c", "3s", "3h", "Jd"]
-- Expected output: "Equal"
-- Test that two identical straight flush hands are equal:
-- Input: ["2h", "3h", "4h", "5h", "6h"], ["2d", "3d", "4d", "5d", "6d"]
-- Expected output: "Equal"
-- Test that a hand with all identical cards is a valid hand:
-- Input: ["3h", "3h", "3h", "3h", "3h"], ["2d", "2d", "2d", "2d", "2d"]
-- Expected output: "Invalid Hand"