# Release 2.8

I have released version 2.8 of org-social.el 🥳

I have focused on improving the user experience with real-time notifications, better post readability, and performance enhancements.

![Screenshot](https://activity.andros.dev/fileserver/01FQ7FH7RWJCEWS8FG7N1W26TX/attachment/original/01K99GN4RBRFYP9CP92YVH3977.jpeg)

## Main features

- **Real-time Desktop Notifications**: Get instant notifications for mentions, reactions, replies, and boosts via SSE (Server-Sent Events). Optional feature, disabled by default via `org-social-realtime-notifications`
- **Post Preview Length Control**: Long posts now show a "Read more" button (default 400 characters). Configurable with `org-social-post-preview-length` or set to `nil` to disable truncation
- **Improved User Mentions**: `org-social-mention-user` now uses async cache system that loads all users from relay without blocking Emacs. Cache updates automatically and silently after timeline is displayed

## Other improvements

- Bug Fix: Poll voting now works correctly with vfile URLs by using local cache file
- UI Enhancement: Thread view displays full posts without truncation for better readability
- Performance: Timeline navigation optimized to avoid unnecessary feed reloads when switching between buffers

## Links

➕ Instructions for updating: https://github.com/tanrax/org-social.el?tab=readme-ov-file#update-version

📝 Issues/bugs/problems: https://github.com/tanrax/org-social.el/issues

📜 Source Code: https://github.com/tanrax/org-social.el

❤️ Support the project! https://liberapay.com/org-social/

Enjoy!
