'use client'

import { useEffect, useRef } from 'react'

type VideoPlayerProps = {
    src: string;
};

export default function VideoPlayer(video: VideoPlayerProps) {
    const videoRef = useRef<HTMLVideoElement | null>(null)

    useEffect(() => {
        let player: any

        const initDash = async () => {
            const dashjs = await import('dashjs')
            player = dashjs.MediaPlayer().create()
            player.initialize(videoRef.current, video.src, true)
        }

        if (videoRef.current) {
            initDash()
        }
        
        return () => {
            player?.reset()
        }
    }, [video])

    return (
        <video
            ref={videoRef}
            controls
            style={{ width: '100%', maxWidth: '800px' }}
        />
    );
}