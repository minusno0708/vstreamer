import { VideoProps } from '.';

export default function VideoCard(video: VideoProps) {
    return (
        <div>
            <li key={video.id}>{video.name}</li>
        </div>
    )
}